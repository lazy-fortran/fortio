"""Independent h5py inputs for both legacy and modern filtered headers."""
import sys

import h5py
import numpy as np


def flip_payload_byte(path, dataset_name):
    """Corrupt one payload byte of the only chunk, leaving its checksum stale."""
    with h5py.File(path, "r") as handle:
        info = handle[dataset_name].id.get_chunk_info(0)
        offset = info.byte_offset
    with open(path, "r+b") as raw:
        raw.seek(offset + 8)
        byte = raw.read(1)
        raw.seek(offset + 8)
        raw.write(bytes([byte[0] ^ 0x01]))


for version in ("earliest", "latest"):
    for shuffle in (False, True):
        path = f"{sys.argv[1]}-{version}-{int(shuffle)}.h5"
        values = np.arange(1, 9, dtype=np.float64)
        with h5py.File(path, "w", libver=version) as handle:
            if version == "earliest":
                handle.create_dataset(
                    "unfiltered_integer", data=np.array([3, -17, 42], dtype=np.int32),
                    chunks=(3,),
                )
                handle.create_dataset("unfiltered_vector", data=values, chunks=(8,))
                handle.create_dataset(
                    "unfiltered_matrix",
                    data=np.arange(1, 513, dtype=np.float64).reshape(16, 32),
                    chunks=(16, 32),
                )
            handle.create_dataset(
                "integer", data=np.array([3, -17, 42], dtype=np.int32),
                chunks=(3,), compression="gzip", shuffle=shuffle,
            )
            handle.create_dataset(
                "matrix", data=np.arange(1, 513, dtype=np.float64).reshape(16, 32),
                chunks=(16, 32), compression="gzip", shuffle=shuffle,
            )
            handle.create_dataset(
                "multichunk", data=np.arange(64, dtype=np.int32),
                chunks=(8,), compression="gzip",
            )
            handle.create_dataset(
                "fletcher", data=values, chunks=(8,), fletcher32=True,
            )
            handle.create_dataset(
                "fletcher_integer", data=np.array([3, -17, 42], dtype=np.int32),
                chunks=(3,), fletcher32=True,
            )
            handle.create_dataset(
                "fletcher_corrupt", data=values, chunks=(8,), fletcher32=True,
            )
            handle.create_dataset(
                "fletcher_skipped", data=values, chunks=(8,), fletcher32=True,
            )
            handle["fletcher_skipped"].id.write_direct_chunk(
                (0,), values.tobytes(), filter_mask=0x1,
            )
            handle.create_dataset(
                "pipeline_skipped", data=values, chunks=(8,),
                compression="gzip", shuffle=True, fletcher32=True,
            )
            _, stored = handle["pipeline_skipped"].id.read_direct_chunk((0,))
            handle["pipeline_skipped"].id.write_direct_chunk(
                (0,), stored[:-4], filter_mask=0x4,
            )
        flip_payload_byte(path, "fletcher_corrupt")
        with h5py.File(path, "r") as handle:
            if version == "earliest":
                for name in ("unfiltered_integer", "unfiltered_vector", "unfiltered_matrix"):
                    assert handle[name].id.get_create_plist().get_nfilters() == 0
                    assert handle[name].id.get_num_chunks() == 1
            assert handle["fletcher"].id.get_chunk_info(0).filter_mask == 0
            assert handle["fletcher_skipped"].id.get_chunk_info(0).filter_mask == 1
            assert handle["pipeline_skipped"].id.get_chunk_info(0).filter_mask == 4
            assert list(handle["pipeline_skipped"].filter_ids) == [2, 1, 3]
            np.testing.assert_array_equal(handle["fletcher"][()], values)
            np.testing.assert_array_equal(handle["fletcher_skipped"][()], values)
            np.testing.assert_array_equal(handle["pipeline_skipped"][()], values)
            try:
                handle["fletcher_corrupt"][()]
            except OSError:
                pass
            else:
                raise AssertionError("h5py accepted a corrupt fletcher32 chunk")
